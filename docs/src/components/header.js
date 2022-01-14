import { Link } from "gatsby";
import PropTypes from "prop-types";
import React from "react";

const headerLink = {
  color: "hsl(211deg 23% 20%)",
  textDecoration: "none",
  padding: "1rem 1rem",
  fontWeight: 600,
};

const Header = ({ siteTitle }) => (
  <div>
    <div
      style={{
        margin: "0 auto",
        padding: "20px 30px",
        display: "flex",
        flexDirection: "row",
        background: "rgb(215 239 253 / 55%)",
        textAlign: "center",
        alignItems: "center",
        position: "relative",
      }}
    >
      <Link to="/" style={headerLink}>
        <h1 style={{ margin: 0 }}>{siteTitle}</h1>
      </Link>
      <Link to="/about" style={headerLink}>
        About
      </Link>
      <Link to="/spec" style={headerLink}>
       Spec
      </Link>
    </div>
  </div>
);

Header.propTypes = {
  siteTitle: PropTypes.string,
};

Header.defaultProps = {
  siteTitle: "",
};

export default Header;
