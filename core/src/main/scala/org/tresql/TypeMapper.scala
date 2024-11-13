package org.tresql
package metadata

import java.sql.Types
import scala.reflect.{Manifest, ManifestFactory}
import scala.collection.immutable.Map

trait TypeMapper {
  private val typeToVendorType:  Map[String, Map[String, String]] = Map(
    "array"        -> Map("postgresql" -> "[]", "sql" -> " array"),
    "boolean"      -> Map("oracle" -> "char", "postgresql" -> "bool", "sql" -> "boolean"),
    "bytes"        -> Map("postgresql" -> "bytea", "sql" -> "blob"),
    "bytes[]"      -> Map("hsqldb" -> s"varbinary(${1024 * 1024 * 1024}) array", "postgresql" -> "bytea[]", "sql" -> "varbinary array"),
    "dateTime"     -> Map("sql" -> "timestamp"),
    "decimal"      -> Map("sql" -> "numeric"),
    "double"       -> Map("hsqldb" -> "float", "sql" -> "double precision"),
    "float"        -> Map("hsqldb" -> "real", "postgresql" -> "real", "sql" -> "float"),
    "int"          -> Map("oracle" -> "numeric(9)", "sql" -> "integer"),
    "integer"      -> Map("sql" -> "numeric"),
    "long"         -> Map("oracle" -> "numeric(18)", "sql" -> "bigint"),
    "short"        -> Map("sql" -> "smallint"),
    "string"       -> Map("sql" -> "clob", "oracle" -> "varchar2", "postgresql" -> "text", "hsqldb" -> "varchar"),
    "text"         -> Map("sql" -> "clob", "oracle" -> "varchar2", "postgresql" -> "text", "hsqldb" -> "longvarchar"),
  )

  def to_sql_type(vendor: String, typeName: String): String =
    if (typeName endsWith "]") // isArray
      if (typeToVendorType contains typeName)
        toVendorType(vendor, typeName)
      else
        s"${to_sql_type(vendor, typeName.substring(0, typeName.indexOf('[')))}${toVendorType(vendor, "array")}"
    else
      toVendorType(vendor, typeName)

  private def toVendorType(vendor: String, typeName: String) =
    typeToVendorType.get(typeName).flatMap(vt => vt.get(vendor).orElse(vt.get("sql"))) getOrElse typeName

  def to_scala_type(typeName: String): String = typeName match {
    case "any"          => "Any"
    case "boolean"      => "java.lang.Boolean"
    case "bytes"        => "Array[Byte]"
    case "date"         => "java.sql.Date"
    case "dateTime"     => "java.sql.Timestamp"
    case "decimal"      => "scala.math.BigDecimal"
    case "double"       => "scala.math.BigDecimal"
    case "float"        => "scala.math.BigDecimal"
    case "int"          => "java.lang.Integer"
    case "integer"      => "java.lang.Long"
    case "long"         => "java.lang.Long"
    case "short"        => "java.lang.Integer"
    case "string"       => "java.lang.String"
    case "text"         => "java.lang.String"
    case "time"         => "java.sql.Time"
    case "timestamp"    => "java.sql.Timestamp"
    case "array"        => "java.sql.Array"
    case "unit"         => "Unit"
    case _              => "Any"
  }

  def from_jdbc_type(jdbcTypeCode: Int): String = jdbcTypeCode match {
    case Types.ARRAY                    => "array"
    case Types.BIGINT                   => "long"
    case Types.BINARY                   => "bytes"
    case Types.BIT                      => "boolean"
    case Types.BLOB                     => "bytes"
    case Types.BOOLEAN                  => "boolean"
    case Types.CHAR                     => "string"
    case Types.CLOB                     => "string"
    case Types.DATALINK                 => "string"
    case Types.DATE                     => "date"
    case Types.DECIMAL                  => "decimal"
    case Types.DISTINCT                 => "string"
    case Types.DOUBLE                   => "double"
    case Types.FLOAT                    => "double"
    case Types.INTEGER                  => "int"
    case Types.JAVA_OBJECT              => "bytes"
    case Types.LONGNVARCHAR             => "string"
    case Types.LONGVARBINARY            => "bytes"
    case Types.LONGVARCHAR              => "string"
    case Types.NCHAR                    => "string"
    case Types.NCLOB                    => "string"
    case Types.NULL                     => "string"
    case Types.NUMERIC                  => "decimal"
    case Types.NVARCHAR                 => "string"
    case Types.OTHER                    => "any"
    case Types.REAL                     => "float"
    case Types.REF                      => "string"
    case Types.REF_CURSOR               => "string"
    case Types.ROWID                    => "string"
    case Types.SMALLINT                 => "short"
    case Types.SQLXML                   => "string"
    case Types.STRUCT                   => "bytes"
    case Types.TIME                     => "time"
    case Types.TIME_WITH_TIMEZONE       => "time"
    case Types.TIMESTAMP                => "dateTime"
    case Types.TIMESTAMP_WITH_TIMEZONE  => "dateTime"
    case Types.TINYINT                  => "short"
    case Types.VARBINARY                => "bytes"
    case Types.VARCHAR                  => "string"
    case x => sys.error("Unexpected jdbc type code: " + x)
  }
}

object TypeMapper {
  def scalaToJdbc(scalaTypeName: String): Int = scalaTypeName match {
    case "Array[Byte]"              => Types.VARBINARY
    case "Boolean" | "boolean"      => Types.BOOLEAN
    case "Double" | "double"        => Types.DOUBLE
    case "Float" | "float"          => Types.REAL
    case "Int" | "int"              => Types.INTEGER
    case "java.io.InputStream"      => Types.LONGVARBINARY
    case "java.io.Reader"           => Types.LONGVARCHAR
    case "java.lang.Boolean"        => Types.BOOLEAN
    case "java.lang.Double"         => Types.DOUBLE
    case "java.lang.Float"          => Types.REAL
    case "java.lang.Integer"        => Types.INTEGER
    case "java.lang.Long"           => Types.BIGINT
    case "java.lang.Short"          => Types.SMALLINT
    case "java.lang.String"         => Types.VARCHAR
    case "java.math.BigDecimal"     => Types.DECIMAL
    case "java.math.BigInteger"     => Types.NUMERIC
    case "java.sql.Blob"            => Types.BLOB
    case "java.sql.Clob"            => Types.CLOB
    case "java.sql.Date"            => Types.DATE
    case "java.sql.Time"            => Types.TIME
    case "java.sql.Timestamp"       => Types.TIMESTAMP
    case "java.sql.Array"           => Types.ARRAY
    case "java.time.LocalDate"      => Types.DATE
    case "java.time.LocalDateTime"  => Types.TIMESTAMP
    case "java.time.LocalTime"      => Types.TIME
    case "java.util.Date"           => Types.TIMESTAMP
    case "Long" | "long"            => Types.BIGINT
    case "scala.math.BigDecimal"    => Types.DECIMAL
    case "scala.math.BigInt"        => Types.NUMERIC
    case "Short" | "short"          => Types.SMALLINT
    case "String"                   => Types.VARCHAR
    case _                          => Types.OTHER
  }
}
