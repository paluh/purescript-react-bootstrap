import React from "react";
import { Alert } from 'react-bootstrap';

export const alertImpl = function(props) {
  return function(children) {
    return React.createElement(Alert, props, ...children);
  };
};
