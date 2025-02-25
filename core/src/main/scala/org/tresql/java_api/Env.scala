package org.tresql.java_api

import java.sql.Connection

import org.tresql._
import org.tresql.metadata.JDBCMetadata
import org.tresql.{ThreadLocalResources => TLR}

trait IdExprFunc { def getIdExpr(table: String): String }
trait LogMessage { def get: String }
trait LogParams { def get: Seq[(String, Any)] }
trait Logger { def log(msg: LogMessage, params: LogParams, topic: LogTopic): Unit }
object Dialects {
  def ANSISQL = dialects.ANSISQLDialect
  def HSQL = dialects.HSQLDialect
  def Oracle = dialects.OracleDialect
}
object Metadata {
  def JDBC(conn: Connection, defaultSchema: String) = JDBCMetadata(conn, defaultSchema)
}

trait ThreadLocalResources extends TLR {
  import CoreTypes._

  var javaLogger: Logger = null

  def getConnection: Connection = conn
  def setConnection(c: Connection): Unit = { conn = c }
  def getDialect: Dialect = dialect
  def setDialect(d: Dialect): Unit = { dialect = d }
  def getToBindableValue: PartialFunction[Any, Any] = toBindableValue
  def setToBindableValue(tbv: PartialFunction[Any, Any]) = { toBindableValue = tbv }
  def getIdExprFunc: IdExprFunc = new IdExprFunc {
    override def getIdExpr(table: String) = idExpr(table)
  }
  def setIdExprFunc(f: IdExprFunc): Unit = { idExpr = f.getIdExpr }
  def getMetadata: Metadata = metadata
  def setMetadata(md: Metadata): Unit = { metadata = md }

  def getCache = cache

  override def logger = (msg, params, topic) =>
    if (javaLogger != null) javaLogger.log(new LogMessage { override def get = msg },
      new LogParams { override def get = params }, topic)

  def setLogger(logger: Logger) = javaLogger = logger

}
